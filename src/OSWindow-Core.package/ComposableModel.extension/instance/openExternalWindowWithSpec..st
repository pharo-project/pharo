*OSWindow-Core
openExternalWindowWithSpec: aSpec
    "Build the widget using the spec name provided as argument and display it into the world"

    ^ self openExternalWindowWithSpecLayout: (self retrieveSpec: aSpec)