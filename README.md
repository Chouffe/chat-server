# HaskChat

HaskChat is a simple chat server (irc like) written in Haskell.

  ```
  _  _         _    ___ _         _
 | || |__ _ __| |__/ __| |_  __ _| |_
 | __ / _` (_-< / / (__| ' \/ _` |  _|
 |_||_\__,_/__/_\_\\___|_||_\__,_|\__|
 ```

HaskChat supports any number of clients and allows them to join and leave at any time.
It is based on the [Stanford Haskell Class: Functional Systems in Haskell](http://www.scs.stanford.edu/14sp-cs240h/labs/lab2.html)

## Features

- TCP communication between clients and server
- The server listen on `CHAT_SERVER_PORT` ENV variable or `12345` (default port)
- Multiple chatrooms are available
- Logging is turned on server side and logs to stdout

## Commands

- `/chatrooms`              displays the list of all chatrooms
- `/help`                   displays this help menu
- `/join <chatroom>`        joins the chatroom <chatroom>
- `/msg <clientId> <msg>`   sends a private message <msg> to client <clientId>
- `/quit`                   exits the chat server
- `/who`                    displays who is in the current chatroom
- `/whoami`                 displays your clientid

## TODO

- Add thorough test suite using `QuickCheck` and `Hspec`.
- Add better concurrency with STMs.
- Add a better client readline with `Haskeline`.
- Add more commands (irc commands).
- Allow clients to pick a username.
