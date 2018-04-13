"
I am the generic cluster for storing and loading variable word objects. Most word-like objects are subclasses from ArrayedCollection. Hence, we may have used the method #writeOn: to serialize, and #newFromStream: to materialize. 

#writeOn: is slow because it ALWAYS encode wors in a big endian format. Most machines today are even little endian. Even worst, #newFromStream: has to always do a #restoreEndianness. 

#newFromStream: is fast because it uses the method #nextWordsInto: which is a hack but that is really fast. The problem is that #nextWordsInto: has to always do the #restoreEndianness when we are in little endian machines (most of the time).

Hence, this cluster provives the best of both worlds: the serialization is done with a platform-dependen way, that is, the endianness will be the same of the machine where we are serializing. In addition, we serialize with the same hack that #nextWordsInto: uses, that is, the Bitmap >> hackBits:
And the materialization will use a variation of #nextWordsInto:  (#fuelNextWordsInto:) that ONLY does the #restoreEndianness if the machine where we are materializing has a different endianness than where we have serialized the object. To do this, while serializing, we store in the header of the Fuel stream, the endianness. Then in materialization, we compare agains that. 

Conclusion: we only restore endianness when needed.

Notice, in addition, that the method #newFromStream: sends the message #restoreEndianness. This method EXPECTS that the object was serialized in big endian format, and it will only change the endianness if we are materializing in a little endian machine. This means that if you serialize in little endian and materialize in big endian, you are screw.  For this reason, this cluster uses a method that always change the endianness, Bitmap class >> swapBytesIn:from:to: 

Future work: maybe we can modify and use the Bitmap hackBits:   not only for materialization but also for serialization. 

"
Class {
	#name : #FLWordObjectCluster,
	#superclass : #FLBitsObjectCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLWordObjectCluster >> materializeInstanceWith: aDecoder [

	| inst wideSize |

	wideSize :=  aDecoder nextEncodedPositiveInteger.
	inst := theClass basicNew: wideSize.	

	aDecoder nextEncodedWordsInto: inst. 

	(aDecoder isBigEndian = FLPlatform current isBigEndian asBit)
		ifTrue: [^ inst ]
		ifFalse: [ ^ self swapBytesOf: inst ] 
	
]

{ #category : #'serialize/materialize' }
FLWordObjectCluster >> serializeInstance: aWordObject with: anEncoder [

	anEncoder encodePositiveInteger: aWordObject basicSize.
	anEncoder encodeWords: aWordObject


]

{ #category : #endianness }
FLWordObjectCluster >> swapBytesOf: aWordObject [
	Bitmap
                swapBytesIn: aWordObject
                from: 1
                to: aWordObject basicSize.
	^ aWordObject
]
