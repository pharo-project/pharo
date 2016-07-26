intention revealing
sizeCheck
	^self createParseTreeRule: (#(#do: #collect: #reject: #select:) collect: 
					[:each | 
					'`@object size > 0 ifTrue: [`@object ' , each
						, ' [:`each | | `@temps | `@.Statements1]. `@.Statements2]'])
				, (#(#do: #collect: #reject: #select:) collect: 
							[:each | 
							'`@object isEmpty ifFalse: [`@object ' , each
								, ' [:`each | | `@temps | `@.Statements1]. `@.Statements2]'])
		name: 'Unnecessary size check'