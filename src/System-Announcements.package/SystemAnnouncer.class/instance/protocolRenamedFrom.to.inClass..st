triggering
protocolRenamedFrom: oldName to: newName inClass: aClass
	self
		announce:
			(ProtocolRenamed new
				classReorganized: aClass;
				oldProtocolName: oldName;
				newProtocolName: newName;
				yourself)