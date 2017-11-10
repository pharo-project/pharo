Extension { #name : #SearchFacade }

{ #category : #'*Versionner-Spec-Browser' }
SearchFacade class >> mcPackageSearch [
	^ self mcPackageSearchRejectAll: [ :aPackage | false ] withInitialFilter: nil
]

{ #category : #'*Versionner-Spec-Browser' }
SearchFacade class >> mcPackageSearchRejectAll: aRejectBlock [
	^ self mcPackageSearchRejectAll: aRejectBlock withInitialFilter: nil
]

{ #category : #'*Versionner-Spec-Browser' }
SearchFacade class >> mcPackageSearchRejectAll: aRejectBlock withInitialFilter: filter [
	| workingCopies |
	
	workingCopies := MCWorkingCopy allManagers asSortedCollection: [ :a :b | a packageName <= b packageName].
	workingCopies := workingCopies reject: aRejectBlock.
	
	^ ListDialogWindow new
				getList: [ :regex | workingCopies select: [ :workingCopy | regex search: workingCopy package name] ];
				displayBlock: [ :workingCopy | workingCopy package name ];
				title: 'Monticello packages';
				initialFilter: (filter ifNil: [ '' ] ifNotNil: [ filter]);
				yourself. 

]

{ #category : #'*Versionner-Spec-Browser' }
SearchFacade class >> projectWithConfigurationSearch [
	"Get a  search widget with filled with projects having a ConfigurationOf in the image."

	^ self projectWithConfigurationSearchRejectAll: [ :item | false ]
]

{ #category : #'*Versionner-Spec-Browser' }
SearchFacade class >> projectWithConfigurationSearchRejectAll: aRejectBlock [
	"Get a  search widget with filled with projects having a ConfigurationOf in the image."
	| projects |

	projects := (MBConfigurationRoot current configurations) reject: aRejectBlock.
	
	^ ListDialogWindow new
				getList: [ :regex | projects select: [ :project | regex search: project name configurationBaseName] ];
				displayBlock: [:e | e name configurationBaseName];
				browseBlock: [:answer | Smalltalk tools browser openOnPackage: answer ]; 
				title: 'Projects with a configuration';
				yourself. 
]
