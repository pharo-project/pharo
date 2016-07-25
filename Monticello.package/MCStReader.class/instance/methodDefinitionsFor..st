reading
methodDefinitionsFor: aRingClass

	^ aRingClass methods collect: 
		[ :each | each asMCMethodDefinition ]