public
open
	| archive |
	self reference exists
		ifFalse: [ ^ self ].
	archive := ZipArchive new.
	self reference
		binaryReadStreamDo: [ :input | 
			archive readFrom: input contents readStream.
			archive members
				do: [ :member | 
					| path |
					path := self pathFromMember: member.
					member isDirectory
						ifTrue: [ self ensureCreateDirectory: path ]
						ifFalse: [ 
							self ensureCreateDirectory: path parent.
							(self createFile: path) member: member ] ] ].
	^ self