transforming
transform
	self setOption: #inlineExpression toUse: [:ref :string | true].
	self getterSetterMethods do: [:each | self inline: each]