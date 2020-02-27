"
I am an expert on Unix filesystem conventions. I resolve origins according to these conventions.
"
Class {
	#name : #UnixResolver,
	#superclass : #PlatformResolver,
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
UnixResolver class >> platformName [
	^  'unix'
]

{ #category : #origins }
UnixResolver >> cache [
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CACHE_HOME' or: [ self home / '.cache' ]
]

{ #category : #origins }
UnixResolver >> desktop [
	^ (self xdgUserDir: 'DESKTOP') ifNil: [ super desktop ]
]

{ #category : #origins }
UnixResolver >> documents [
	^ (self xdgUserDir: 'DOCUMENTS') ifNil: [ super documents ]
]

{ #category : #origins }
UnixResolver >> downloads [
	^ (self xdgUserDir: 'DOWNLOADS') ifNil: [ super downloads ]
]

{ #category : #origins }
UnixResolver >> home [
	^ self directoryFromEnvVariableNamed: 'HOME'
]

{ #category : #origins }
UnixResolver >> preferences [
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CONFIG_HOME' or: [ self home / '.config' ]
]

{ #category : #origins }
UnixResolver >> temp [
	^ '/tmp' asFileReference
]

{ #category : #origins }
UnixResolver >> userData [
	<origin>
	^ self directoryFromEnvVariableNamed: 'XDG_DATA_HOME' or: [ self home / '.local' / 'share' ]
]

{ #category : #helpers }
UnixResolver >> xdgParseUserDirLine: aStream [
	"Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| path firstChar |
	(aStream next = $") ifFalse: [ ^ nil ].
	firstChar := aStream next.
	(#($$ $/) includes: firstChar) ifFalse: [ ^ nil ].
	path := firstChar = $$
				ifTrue: [ (aStream next: 5) = 'HOME/' ifFalse: [ ^ nil ].
					       self home / (aStream upTo: $") ]
				ifFalse: [ self resolveString: '/', (aStream upTo: $") ].
	^ path
]

{ #category : #helpers }
UnixResolver >> xdgUserDir: userDirName [
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
]
