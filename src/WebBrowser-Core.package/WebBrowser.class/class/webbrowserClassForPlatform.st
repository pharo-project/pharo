private - accessing
webbrowserClassForPlatform

  ^ self allSubclasses detect: #isForCurrentSystem ifNone: [ self error: 'No web browser subclass for platform provided' ]