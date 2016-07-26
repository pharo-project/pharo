helpers
xdgUserDir: userDirName
	"Read ~/.config/user-dirs.dirs to find the directory of userDirName (e.g., 'DESKTOP')"
	"http://freedesktop.org/wiki/Software/xdg-user-dirs"
	"This file is written by xdg-user-dirs-update If you want to change or add directories, just edit the line you're interested in. All local changes will be retained on the next run Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| configFile |
	configFile := self preferences / 'user-dirs.dirs'.
	(configFile isFile and: [ configFile isReadable ]) ifFalse: [ ^ nil ].
	configFile readStreamDo: [ :stream | 
		[ stream atEnd ]
			whileFalse: [ 
				((stream peek ~= $#) and: [ (stream upTo: $=) = ('XDG_', userDirName, '_DIR') ])
							ifTrue: [ ^ self xdgParseUserDirLine: stream ]
							ifFalse: [ stream nextLine ] ] ].
	^ nil