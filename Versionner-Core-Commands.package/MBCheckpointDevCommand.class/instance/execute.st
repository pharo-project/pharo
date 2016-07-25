execute
execute
	| message configClass |
	configClass := self configurationClass.
	message := self uiManager
		multilineRequest:
			'Enter commit message for ' , (configClass name configurationBaseName) , ' '
				, (target project version: #development) versionString
		initialAnswer: (target project version: #development) versionString, ' ():'.
	message == nil
		ifTrue: [ ^ self ].
	self executeWithMessage: message
