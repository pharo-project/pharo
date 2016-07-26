*renraku
externalProperties

	^ (Pragma allNamed: #eProperty from: self class to: ProtoObject) flatCollect: [ :pragma |
		pragma method valueWithReceiver: self arguments: #() ]
