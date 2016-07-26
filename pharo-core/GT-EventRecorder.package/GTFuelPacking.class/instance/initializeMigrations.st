initialization
initializeMigrations
	| pragmas |
	pragmas := Pragma allNamed: #fuelMaterializerMigration in: self class.
	migrations := pragmas 
						collect: [ :eachPragma | self perform: eachPragma selector ] 
						as: OrderedCollection.