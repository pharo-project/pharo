tests
testStoring

	self	
		assert: time storeString = '''4:02:47 am'' asTime';
		assert: time = ('4:02:47 am' asTime).
