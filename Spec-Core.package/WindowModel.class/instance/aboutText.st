api
aboutText
	"if my aboutText has not been set the fallback is:
	  - if I do have a model, use its' about text
	  - else use the behavior defined in super
	"

	^aboutText value
		ifNil: [self model
			ifNil: [ super aboutText ]
			ifNotNil: [:m | m aboutText ]]
		ifNotNil: [ aboutText value ]