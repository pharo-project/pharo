private
compileInTarget
	| methodSelector source |
	source := self method methodClass theNonMetaClass
		perform: self method selector
		withArguments: (1 to: self method selector numArgs) asArray.
	"actual selector may be different from the one used to store the source"
	methodSelector := self targetClass compile: source.
	self targetClass organization
		classify: self selector
		under: self extensionProtocolName