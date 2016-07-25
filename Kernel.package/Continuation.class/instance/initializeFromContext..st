private
initializeFromContext: aContext
	| valueStream context |
	valueStream := WriteStream on: (Array new: 20).
	context := aContext.
	[context notNil] whileTrue:
		[valueStream nextPut: context.
		1 to: context class instSize do: [:i | valueStream nextPut: (context instVarAt: i)].
		1 to: context size do: [:i | valueStream nextPut: (context at: i)].
		context := context sender].
	values := valueStream contents