accessing
sharedPools

	^ classDefinition instanceSide sharedPools collect: [ :pool | 
		PBClassDefinition on: pool environment: bootstrapEnvironment ]