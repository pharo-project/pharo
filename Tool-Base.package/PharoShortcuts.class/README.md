I am a class defining the current shortcuts of the system. My main purpose is to avoid the duplication of shortcut definition amongst basic tools and to allow to change the shortcuts from a single place.

Users may query me to know the key combinations used to accept or cancel an action, browse or inspect an object.

EXAMPLES

PharoShortcuts current browseShortcut => Cmd-B

ATTENTION - I'm not still finished. My #current method returns always a new instance of myself instead of a configured one.

NEXT Steps: Allow one to configure the shortcuts and to define several shortcut configurations via settings.