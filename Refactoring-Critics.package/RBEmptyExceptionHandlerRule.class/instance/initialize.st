initialization
initialize
	super initialize.
	self matcher 
		matches: '`@block 
			on: `{ :node |
				| class |
				node isVariable 
					and: [ (class := Smalltalk classNamed: node name) notNil
					and: [ (class includesBehavior: Exception)
					and: [ (class includesBehavior: Notification) not ] ] ] }
			do: [ :`@err | | `@temps | ]'
		do: [ :node :answer | node ]