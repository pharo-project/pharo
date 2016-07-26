instance creation
xml: string
	^ (self type: ZnMimeType applicationXml)
		string: string;
		yourself