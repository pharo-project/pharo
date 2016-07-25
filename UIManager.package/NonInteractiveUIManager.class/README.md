This is a non-interactive UI manager, i.e. a UI manager which doesn't provides any kind of interactivity with users.

For most of requests, it throws an ErrorNonInteractive exception, which can be handled by various tools to do things differently when UI is not avaliable. For example:

response := [ UIManager default request: 'what is your name?' ] on: ErrorNonInteractive do: [:ex | ex resume: 'Mr. John Smith' ].

You can replace the default UI Manager with my instance in cases, when you need to guarantee that your task(s) will run in fully automated mode. This is useful for things like:
  - when image runs as a persistent application on a server
  - image runs headless from command-line with some batch scripts/commands

