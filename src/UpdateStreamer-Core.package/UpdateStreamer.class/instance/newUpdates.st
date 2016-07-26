private - actions
newUpdates
	"Return a list of fully formed URLs of update files we do not yet have up to the upToNumber instance variable if set." 

	"self new newUpdates"

	| result |
	
	result := ZnClient new
		systemPolicy;
		setAcceptEncodingGzip;
		beOneShot;
		accept: ZnMimeType text;
		url: self updateListFileURLString;
		ifFail: [ :exception | self error: 'Cannot access update server: ' , exception printString ];
		get.
		
	^ self newUpdatesFromString: result.