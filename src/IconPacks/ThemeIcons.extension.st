Extension { #name : 'ThemeIcons' }

{ #category : '*IconPacks' }
ThemeIcons >> downloadFromUrl [

	self class destinationPath ensureCreateDirectory.
	zipArchive := self class destinationPath / (self name, '.zip').
	zipArchive exists
		ifFalse: [
			ZnClient new
				url: self url;
				downloadTo: zipArchive ].
	^ zipArchive
]

{ #category : '*IconPacks' }
ThemeIcons class >> loadDefault [
	<script>
	^ self new
		  name: self defaultIconsPack;
		  loadIconsFromUrl;
		  yourself
]

{ #category : '*IconPacks' }
ThemeIcons >> loadIconsFromUrl [

	^ self loadIconsFromUrlUsingScale: 1
]

{ #category : '*IconPacks' }
ThemeIcons >> loadIconsFromUrlUsingScale: newScale [

	self downloadFromUrl.
	^ self loadIconsFromZipArchiveUsingScale: newScale
]
