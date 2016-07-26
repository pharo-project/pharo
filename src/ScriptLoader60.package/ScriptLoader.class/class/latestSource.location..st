source
latestSource: packageName location: location

  | contents versionName zip |
  
  versionName := location, '/', (self latestVersionOf: packageName location: location).
  contents := ZnClient new
		systemPolicy;
		setAcceptEncodingGzip;
		beOneShot;
		url: versionName;
		ifFail: [ :exception | self error: 'Cannot access latest source: ' , exception printString ];
		get.
    
  zip := ZipArchive new.
  zip readFrom: contents readStream.
  ^  (zip memberNamed: 'snapshot/source.st') contents.