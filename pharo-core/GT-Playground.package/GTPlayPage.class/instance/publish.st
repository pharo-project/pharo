actions
publish
	(self confirm: 'Publish playground content to the cloud?')
        ifFalse: [ ^ self ].
    self content ifEmpty: [ 
        self inform: 'Nothing was published because the playground is empty'.
        ^ self ].
    Clipboard clipboardText: (publishedUrl := (GTUrlProvider new post: self contentString) asString).
    self inform: publishedUrl , ' was published and the url was copied to clipboard'