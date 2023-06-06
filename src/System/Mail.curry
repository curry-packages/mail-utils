---------------------------------------------------------------------------
--- This library contains operations for sending emails.
--- The implementation might need to be adapted to the local
--- environment.
---
--- @author Michael Hanus
--- @version June 2023
---------------------------------------------------------------------------

module System.Mail
  ( MailOption(..), sendMail, sendMailWithOptions
  , showSendMail, showSendMailWithOptions
  )
 where

import Control.Monad    ( unless )
import Data.List        ( intercalate )
import System.IO        ( hPutStrLn, stderr )

import System.IOExts    ( evalCmd )
import System.Path      ( fileInPath )

---------------------------------------------------------------------------
--- Options for sending emails.
--- @cons CC  - recipient of a carbon copy
--- @cons BCC - recipient of a blind carbon copy
--- @cons TO  - recipient of the email
data MailOption = CC String | BCC String | TO String
 deriving Eq

---------------------------------------------------------------------------
--- Sends an email via mailx command.
--- @param from - the email address of the sender
--- @param to - the email address of the recipient
--- @param subject - the subject of the email
--- @param contents - the contents of the email
sendMail :: String -> String -> String -> String -> IO ()
sendMail from to subject = sendMailWithOptions from subject [TO to]

--- Sends an email via mailx command and various options.
--- Note that multiple options are allowed, e.g., more than one CC option
--- for multiple recipient of carbon copies.
---
--- Important note: The implementation of this operation is based on the
--- command "mailx" and must be adapted according to your local environment!
---
--- @param from - the email address of the sender
--- @param subject - the subject of the email
--- @param options - send options, e.g., multiple recipients
--- @param contents - the contents of the email
sendMailWithOptions :: String -> String -> [MailOption] -> String -> IO ()
sendMailWithOptions from subject options contents = do
  mailcmdexists <- fileInPath "mailx"
  if mailcmdexists
    then
      -- if mailx has the option -r:
      --execMailCmd ("mailx -n -r \"" ++ from ++ "\" -s \"" ++ subject++"\" "++
      -- if mailx has the option -a:
      execMailCmd "mailx"
        (["-n", "-a", "From: " ++ from, "-s", subject] ++ ccs ++ bccs ++ tos)
        contents
    else error "Command 'mailx' not found in path!"
 where
  tos  = [ s | TO  s <- options ]
  ccs  = concatMap (\m -> ["-a", "Cc: "  ++ m]) [ s | CC  s <- options ]
  bccs = concatMap (\m -> ["-a", "Bcc: " ++ m]) [ s | BCC s <- options ]


--- Executes a command to send an email and pass the contents via stdin.
--- Note that \r characters in the contents are removed due to problems
--- with such contents in some Unix environments.
execMailCmd :: String -> [String] -> String -> IO ()
execMailCmd cmd args contents = do
  (rc,out,err) <- evalCmd cmd args (filter isUnixChar contents)
  unless (rc == 0) $ putStrLn "ERROR during sending email!"
  unless (null out) $ putStrLn out
  unless (null err) $ hPutStrLn stderr err
 where
  isUnixChar c = c /= '\r'

---------------------------------------------------------------------------
-- Operations for testing and debugging.

--- Shows the text (wth subject, sender, and recipient) of an email
--- to be sent with the command 'sendMailWithOptions' but actually
--- does not send it.
--- This is useful for debugging or testing before really sending emails.
---
--- @param from - the email address of the sender
--- @param to - the email address of the recipient
--- @param subject - the subject of the email
--- @param contents - the contents of the email
showSendMail :: String -> String -> String -> String -> String
showSendMail from to subject = showSendMailWithOptions from subject [TO to]

--- Shows the text (wth subject, sender, and recipient) of an email
--- to be sent with the command 'sendMailWithOptions' but actually
--- does not send it.
--- This is useful for debugging or testing before really sending emails.
---
--- @param from - the email address of the sender
--- @param subject - the subject of the email
--- @param options - send options, e.g., multiple recipients
--- @param contents - the contents of the email
showSendMailWithOptions :: String -> String -> [MailOption] -> String -> String
showSendMailWithOptions from subject options contents = unlines $
  [ "From: " ++ from
  , "To  : " ++ intercalate ", " tos ] ++
  (if null ccs  then [] else ["Cc  : " ++ intercalate ", " ccs ]) ++
  (if null bccs then [] else ["Bcc : " ++ intercalate ", " bccs]) ++
  [ "Subject: " ++ subject, "", contents, "" ]
 where
  tos  = [ s | TO  s <- options ]
  ccs  = [ s | CC  s <- options ]
  bccs = [ s | BCC s <- options ]

---------------------------------------------------------------------------
