system v abi
sysVAMD64PostProcess
	| result |
	result := self class new.
	result
		size: size;
		alignment: alignment.
	result reduce: fields inGroupsOfSize: 8 with: [ :left :right |
		self combineSysVAMD64RegisterClass: left registerClass with: right registerClass
	].

	"Post merger cleanup"
	
	"If there is any memory field, pass the whole structure in memory."
	result fields detect: [ :field | field registerClass == #memory] ifFound: [
		result fields: { FFIExternalStructureFlatLayoutField new
			registerClass: #memory;
			offset: 0;
			size: size;
			alignment: alignment;
			yourself
		}.
		^ result
	].

	result countRegisters.
	^ result
	