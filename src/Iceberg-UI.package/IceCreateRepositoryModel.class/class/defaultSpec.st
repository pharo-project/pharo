specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed newColumn:
		[ :col | self buildDefaultSpec: col ]
