initialization
initializeMaterializer: aFLMaterializer 
	self migrations do: [ :eachMigration | eachMigration value: aFLMaterializer ]