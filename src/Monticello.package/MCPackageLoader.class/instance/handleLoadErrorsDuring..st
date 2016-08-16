private
handleLoadErrorsDuring: aBlock
	aBlock on: ClassBuilderWarning do: [ :error | error resume ]