list
addSpecialFields: elements
	
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'all temp vars' 
		value: [ String streamContents: [ :s | 
					self object tempNames withIndexDo: [ :name :index | 
						s 
							nextPutAll: name ;
							nextPut: Character space ;
							nextPut: $: ;
							nextPut: Character space ;
							nextPutAll: (self object namedTempAt: index) asString ;
							nextPut: Character lf ] ] ]).