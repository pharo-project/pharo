*Versionner-Spec-Browser
projectWithConfigurationSearchRejectAll: aRejectBlock
	"Get a  search widget with filled with projects having a ConfigurationOf in the image."
	| projects |

	projects := (MBConfigurationRoot current configurations) reject: aRejectBlock.
	
	^ ListDialogWindow new
				getList: [ :regex | projects select: [ :project | regex search: project name configurationBaseName] ];
				displayBlock: [:e | e name configurationBaseName];
				browseBlock: [:answer | Smalltalk tools browser openOnPackage: answer ]; 
				title: 'Projects with a configuration';
				yourself. 