| bootstrappedPackages pharoRepository targetRepository |

bootstrappedPackages := #(CodeImport 'Ring-Core-Containers' CodeExport 'System-BasicCommandLineHandler'
'Multilingual-Encodings' 'System-SessionManager' 'Collections-Support' 'FileSystem-Disk'
'RPackage-Core' CodeImportCommandLineHandlers 'System-Platforms' RingChunkImporter 
'System-Hashing' 'Ring-Core-Kernel' 'Collections-Unordered' 'Network-UUID' 'Collections-Native'
Slot 'System-Finalization' 'System-Localization' 'System-Support' 'Collections-Arithmetic' 
'Collections-Streams' 'Multilingual-Languages' 'Collections-Stack' 'System-Sources' 
Transcript 'Zinc-Character-Encoding-Core' 'OpalCompiler-Core' 'Collections-Sequenceable'
'Zinc-Resource-Meta-Core' Jobs Kernel 'Collections-Weak' 'Collections-Abstract' 
'Announcements-Core' 'Multilingual-TextConversion' Files 'System-Changes' 'System-Model'
'Collections-Atomic' UIManager 'Collections-Strings' 'System-Announcements' 
'AST-Core' 'System-CommandLine' Traits 'FileSystem-Core' 'System-CommandLineHandler'
'Random-Core' 'NonInteractiveTranscript' 'PharoBootstrap-Initialization' 'Colors').

pharoRepository := MCFileTreeRepository new
	directory: './pharo-core/src' asFileReference;
	yourself.	

targetRepository := MCDirectoryRepository new
	directory: './kernelPackages' asFileReference;
	yourself.
	
bootstrappedPackages do: [ :packageName |
  packageName logCr.
  targetRepository storeVersion: (pharoRepository loadVersionFromFileNamed: packageName, '.package')].	
	
	
    
  
  