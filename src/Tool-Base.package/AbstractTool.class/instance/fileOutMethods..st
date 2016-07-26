method
fileOutMethods: aCollection

	self 
		do: [ :method | method methodClass fileOutMethod: method selector ]
		on: aCollection
		displaying: 'Filing out methods...'.