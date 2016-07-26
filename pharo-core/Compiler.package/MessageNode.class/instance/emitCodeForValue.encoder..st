code generation
emitCodeForValue: stack encoder: encoder
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: true]
		ifFalse: 
			[receiver ~~ nil ifTrue: [receiver emitCodeForValue: stack encoder: encoder].
			arguments do: [:argument | argument emitCodeForValue: stack encoder: encoder].
			pc := encoder methodStreamPosition + 1. "debug pc is first byte of the send, i.e. the next byte".
			selector
				emitCode: stack
				args: arguments size
				encoder: encoder
				super: receiver == NodeSuper]