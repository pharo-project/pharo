baselines
baseline: spec
	<baseline>
	
	spec
		for: #common
		do: [ 
			spec package: 'AST-Core'.
			spec package: 'Announcements-Core'.
			spec package: 'CodeExport'.
			spec package: 'CodeImport'.
			spec package: 'CodeImportCommandLineHandlers'.
			spec package: 'Collections-Abstract'.
			spec package: 'Collections-Arithmetic'.
			spec package: 'Collections-Atomic'.
			spec package: 'Collections-Native'.
			spec package: 'Collections-Sequenceable'.
			spec package: 'Collections-Stack'.
			spec package: 'Collections-Streams'.
			spec package: 'Collections-Strings'.
			spec package: 'Collections-Support'.
			spec package: 'Collections-Unordered'.
			spec package: 'Collections-Weak'.
			spec package: 'Compression'.
			spec package: 'FileSystem-Core'.
			spec package: 'FileSystem-Disk'.
			spec package: 'Files'.
			spec package: 'Jobs'.
			spec package: 'Kernel'.
			spec package: 'Monticello'.
			spec package: 'Multilingual-Encodings'.
			spec package: 'Multilingual-Languages'.
			spec package: 'Multilingual-TextConversion'.
			spec package: 'Network-UUID'.
			spec package: 'OpalCompiler-Core'.
			spec package: 'Random-Core'.
			spec package: 'RPackage-Core'.
			spec package: 'Ring-Core-Containers'.
			spec package: 'Ring-Core-Kernel'.
			spec package: 'Ring-Monticello'.
			spec package: 'RingChunkImporter'.
			spec package: 'Slot'.
			spec package: 'System-Announcements'.
			spec package: 'System-BasicCommandLineHandler'.
			spec package: 'System-Changes'.
			spec package: 'System-CommandLine'.
			spec package: 'System-CommandLineHandler'.
			spec package: 'System-Finalization'.
			spec package: 'System-Hashing'.
			spec package: 'System-Localization'.
			spec package: 'System-Model'.
			spec package: 'System-Platforms'.
			spec package: 'System-SessionManager'.
			spec package: 'System-Sources'.
			spec package: 'System-Support'.
			spec package: 'Traits'.
			spec package: 'Transcript'.
			spec package: 'UIManager'.
			spec package: 'Zinc-Character-Encoding-Core'.
			spec package: 'Zinc-Resource-Meta-Core'.
			
			spec package: 'PharoBootstrap-Initialization' with: [ spec repository: 'http://www.smalltalkhub.com/mc/Pharo/Bootstrap/main' ].
			spec package: 'NonInteractiveTranscript'.
			
			"Tests"
			spec package: 'AST-Tests-Core' with: [ spec requires: #( 'AST-Core' )].
			spec package: 'Announcements-Tests-Core' with: [ spec requires: #( 'Announcements-Core' )].
			spec package: 'Collections-Tests' with: [ spec requires: #(
				'Collections-Abstract'
				'Collections-Arithmetic'
				'Collections-Atomic'
				'Collections-Native'
				'Collections-Sequenceable'
				'Collections-Stack'
				'Collections-Streams'
				'Collections-Strings'
				'Collections-Support'
				'Collections-Unordered'
				'Collections-Weak' )].
			spec package: 'Compression-Tests' with: [ spec requires: #( 'Compression' )].
			spec package: 'FileSystem-Tests-Core' with: [ spec requires: #( 'FileSystem-Core' )].
			spec package: 'FileSystem-Tests-Disk' with: [ spec requires: #( 'FileSystem-Disk' )].
			spec package: 'Files-Tests' with: [ spec requires: #( 'Files' )].
			spec package: 'JobsTests' with: [ spec requires: #( 'Jobs' )].
			spec package: 'Kernel-Tests' with: [ spec requires: #( 'Kernel' ) ].
			spec package: 'Monticello-Tests' with: [ spec requires: #( 'Monticello' 'MonticelloMocks' ) ].
			spec package: 'MonticelloMocks'.
			spec package: 'Multilingual-Tests' with: [ spec requires: #( 'Multilingual-Encodings' 'Multilingual-Languages' 'Multilingual-TextConversion' ) ].
			spec package: 'OpalCompiler-Tests' with: [ spec requires: #( 'OpalCompiler-Core') ].
			spec package: 'RPackage-Tests' with: [ spec requires: #( 'RPackage-Core' )].
			spec package: 'Random-Tests' with: [ spec requires: #( 'Random-Core' )].
			spec package: 'Ring-Tests-Containers' with: [ spec requires: #( 'Ring-Core-Containers' )].
			spec package: 'Ring-Tests-Kernel' with: [ spec requires: #( 'Ring-Core-Kernel' )].
			spec package: 'Ring-Tests-Monticello' with: [ spec requires: #( 'Ring-Monticello' )].
			spec package: 'Slot-Tests' with: [ spec requires: #( 'Slot' )].
			spec package: 'System-Localization-Tests' with: [ spec requires: #( 'System-Localization' )].
			spec package: 'System-SessionManager-Tests' with: [ spec requires: #( 'System-SessionManager' )].
			spec package: 'System-SupportTests' with: [ spec requires: #( 'System-Support' )].
			spec package: 'Tests' with: [ spec requires: #( 'Traits' 'CodeImport' )].
			spec package: 'Zinc-Character-Encoding-Tests' with: [ spec repository: 'http://mc.stfx.eu/ZincHTTPComponents' ].
			spec package: 'Zinc-Resource-Meta-Tests'  with: [ spec repository: 'http://mc.stfx.eu/ZincHTTPComponents' ].
			
			
			"Groups"
			spec group: 'tests-minimal' with: { 'AST-Tests-Core'.
			 'Announcements-Tests-Core'.
			 'Collections-Tests'.
			 'FileSystem-Tests-Core'.
			 'FileSystem-Tests-Disk'.
			 'Files-Tests'.
			 'JobsTests'.
			 'Kernel-Tests'.
			 'Multilingual-Tests'.
			 'OpalCompiler-Tests'.
			 'RPackage-Tests'.
			 'Random-Tests'.
			 'Ring-Tests-Containers'.
			 'Ring-Tests-Kernel'.
			 'Slot-Tests'.
			 'System-Localization-Tests'.
			 'System-SessionManager-Tests'.
			 'System-SupportTests'.
			 'Tests'.
			 'Zinc-Character-Encoding-Tests'.
			 'Zinc-Resource-Meta-Tests'}.
			
			spec group: 'tests' with: #( 'tests-minimal' 'Compression-Tests' 'Monticello-Tests' 'Ring-Tests-Monticello' ).
			spec group: 'full-minimal' with: #( 'minimal' 'tests-minimal').
			spec group: 'full-default' with: #( 'default' 'tests').
			
			spec group: 'default' with: #( 'minimal' 'Monticello' 'Ring-Monticello' 'Compression').
			
			spec group: 'minimal' with: #(#CodeImport #'Ring-Core-Containers' #CodeExport #'System-BasicCommandLineHandler' #'Multilingual-Encodings' #'System-SessionManager' #'Collections-Support' #'FileSystem-Disk' #'RPackage-Core' #CodeImportCommandLineHandlers #'System-Platforms' #RingChunkImporter #'System-Hashing' #'Ring-Core-Kernel' #'Collections-Unordered' #'Network-UUID' #'Collections-Native' #Slot #'System-Finalization' #'System-Localization' #'System-Support' #'Collections-Arithmetic' #'Collections-Streams' #'Multilingual-Languages' #'Collections-Stack' #'System-Sources' #Transcript #'Zinc-Character-Encoding-Core' #'OpalCompiler-Core' #'Collections-Sequenceable' #'Zinc-Resource-Meta-Core' #Jobs #Kernel #'Collections-Weak' #'Collections-Abstract' #'Announcements-Core' #'Multilingual-TextConversion' #Files #'System-Changes' #'System-Model' #'Collections-Atomic' #UIManager #'Collections-Strings' #'System-Announcements' #'AST-Core' #'System-CommandLine' #Traits #'FileSystem-Core' #'System-CommandLineHandler' #'Random-Core' 'NonInteractiveTranscript' 'PharoBootstrap-Initialization').
	].