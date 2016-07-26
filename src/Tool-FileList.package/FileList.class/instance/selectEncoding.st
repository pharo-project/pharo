private
selectEncoding
	"self new selectEncoding"
	
	| aMenu  |
	aMenu := UIManager default 
				newMenuIn: self for: self.
				
	TextConverter allSubclasses do: [:each | | names |
		names := each encodingNames.
		names notEmpty ifTrue: [ | label |
			label := '' writeStream.
			names 
				do: [ :eachName | label nextPutAll: eachName ] 
				separatedBy: [ label nextPutAll: ', '].
			aMenu  "add: label contents action: names first asSymbol"
				add: label contents
				target: self
				selector: #fileEncoding: 
				argument: names first asSymbol ]].
	
	aMenu popUpInWorld