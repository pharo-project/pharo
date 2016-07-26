private
class: aClass selector: aSelector method: aMethod to: examples
	(aMethod pragmas 
		detect: [ :pragma | pragma isGTExamplePragma ]
		ifNone: [ nil ]) ifNotNil: [ :pragma |
			self example: (self class: aClass selector: aSelector method: aMethod pragma: pragma) to: examples ] 