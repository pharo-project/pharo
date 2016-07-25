Provides a simple, platform-independent, file stream. I am 
   - bynary
   - not buffered
   - provide no encoding/conversions

!Examples of usage

"Creating a file"
stream := (File named: 'asd.txt' asFileReference fullName) readStream.

"Accessing the stream properties"
stream position.
stream atEnd.

"Writing"
stream nextPut: 17.
stream nextPutAll: 'sdd'.

"Reading"
stream next.
stream next: 2.

"Skipping"
stream skip: 2. 

"reading up to something"
stream upTo: 23.
stream upToAnyOf: #[ 13 30 ].

"peeking"
stream peek.