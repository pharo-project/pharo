file paths
guessWindowsFontFolderPath
	"Guess the location of the Windows font folder"
	| possibilities |

	possibilities := Set new.
	'cdefghijklmnopqrstuvwxyz' do:[:drive |
		#('windows\fonts' 'winnt\fonts') do:[:path | | d |
			(d := (FileLocator driveNamed: drive asSymbol) resolve: path) exists
				ifTrue:[possibilities add: d]]].
	possibilities := possibilities asSortedCollection: [:a :b | a entry creationTime >= b  entry creationTime].
	possibilities ifNotEmpty:[ ^ possibilities first fullName ].
	^nil
	
		 	
			