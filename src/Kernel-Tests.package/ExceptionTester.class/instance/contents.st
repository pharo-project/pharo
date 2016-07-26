logging
contents

	^( self log
		inject: (String new: 80) writeStream
		into: 
			[:result :item |
			result 
				cr; 
				nextPutAll: item;
				yourself] ) contents