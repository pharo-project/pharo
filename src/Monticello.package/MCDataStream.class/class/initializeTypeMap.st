initialize-release
initializeTypeMap
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"

	"MCDataStream initialize"

	TypeMap := WeakKeyDictionary new: 80.	"sparse for fast hashing"
	ReadSelectors := WeakValueDictionary new: 80.
	WriteSelectors := WeakValueDictionary new: 80.

	"These are all the type ids used in serialization and deserealization. The Missing types id are optional for Monticello, see package 
	Monticello-OldDataStreamCompatibility for the registering of the older types ids."
	self
		registerClass: UndefinedObject
		atIndex: 1
		usingReadSelector: #readNil
		usingWriteSelector: #writeNil:.
	self
		registerClass: True
		atIndex: 2
		usingReadSelector: #readTrue
		usingWriteSelector: #writeTrue:.
	self
		registerClass: False
		atIndex: 3
		usingReadSelector: #readFalse
		usingWriteSelector: #writeFalse:.
	self
		registerClass: SmallInteger
		atIndex: 4
		usingReadSelector: #readInteger
		usingWriteSelector: #writeInteger:.
	self
		registerClass: ByteSymbol
		atIndex: 6
		usingReadSelector: #readSymbol
		usingWriteSelector: #writeSymbol:.
	self
		registerClass: Array
		atIndex: 8
		usingReadSelector: #readArray
		usingWriteSelector: #writeArray:.
	"Type id 9 is special."
	self registerReaderSelector: #readInstance atIndex: 9.
	self registerWriterSelector: #writeInstance: atIndex: 9.
	
	self
		registerClass: ByteString
		atIndex: 17
		usingReadSelector: #readString
		usingWriteSelector: #writeString:.
		
	self registerClass: WideString
		atIndex: 20
		usingReadSelector: #readWordLike
		usingWriteSelector: #writeWordLike: