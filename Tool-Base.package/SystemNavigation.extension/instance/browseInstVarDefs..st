*Tool-Base
browseInstVarDefs: aClass

	^ self chooseInstVarFrom: aClass thenDo: 
		[:aVar | self browseAllStoresInto: aVar from: aClass]