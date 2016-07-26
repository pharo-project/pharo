private - accessing file streams
readStreamAtFileIndex: index atPosition: position ifPresent: presentBlock ifAbsent: absentBlock
 
   | queue stream rofa result |
       
  	queue := readOnlyQueue.
  	rofa := queue nextOrNil ifNil: [ self createReadOnlyFiles ].
       
   rofa ifNil: [ ^ absentBlock value ].  

   stream := rofa at: index.

   position > (files at: index) size ifTrue: [ 
		self finishedReading: rofa from: queue. 
		^ absentBlock value 
	].

   stream position: position.
   result := presentBlock value: stream.
       
	self finishedReading: rofa from: queue. 
                      
    ^ result
