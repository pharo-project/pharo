code generation
emitCodeForEffect: stack encoder: encoder
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: false]
		ifFalse: 
			[super emitCodeForEffect: stack encoder: encoder]