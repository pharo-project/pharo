accessing
debugShowDamage
	^ DebugShowDamage ifNil: [DebugShowDamage := false]