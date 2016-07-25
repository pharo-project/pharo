private
allPragmas

	^ self allPragmasIn: (self behavior ifNil: [ ^ #() ])