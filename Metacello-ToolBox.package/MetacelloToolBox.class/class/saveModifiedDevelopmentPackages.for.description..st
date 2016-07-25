scripts
saveModifiedDevelopmentPackages: developmentVersionString for: configurationBasename description: commitMessage
  "Save modified mcz files associated with the project. Then update given version to reflect the new mcz file versions."

  <apiDocumentation>
  | toolbox modifiedPackages cacheRepository |
  modifiedPackages := Dictionary new.
  (toolbox := self configurationNamed: configurationBasename)
    modifiedPackageSpecs: developmentVersionString
    packageSpecsDo: [ :versionSpec :packageSpec :monticelloWorkingCopy | 
      | repositories repositoryGroup wcRepositoryGroup |
      wcRepositoryGroup := monticelloWorkingCopy repositoryGroup.
      repositoryGroup := MCRepositoryGroup new.
      repositories := packageSpec repositories
        collect: [ :each | each createRepository ].
      repositories isEmpty
        ifTrue: [ 
          repositories := versionSpec repositories
            collect: [ :repoSpec | repoSpec createRepository ].
          wcRepositoryGroup repositories notEmpty
            ifTrue: [ 
              repositories := repositories
                select: [ :repo | wcRepositoryGroup includes: repo ] ].
          repositories isEmpty
            ifTrue: [ 
              repositories := monticelloWorkingCopy repositoryGroup repositories
                reject: [ :repo | repo = MetacelloPlatform current defaultPackageCache ] ] ].
      repositories
        do: [ :repo | 
          (monticelloWorkingCopy possiblyNewerVersionsIn: repo) notEmpty
            ifTrue: [ 
              self
                notify:
                  'There are possibly newer versions of the package '
                    , packageSpec name printString , ' in the repository '
                    , repo description printString
                    ,
                      '. Cancel and manually merge if you want to pick up the changes from the later version.' ].
          repositoryGroup addRepository: repo ].
      modifiedPackages at: packageSpec name put: repositoryGroup ].
  cacheRepository := MetacelloPlatform current defaultPackageCache.
  modifiedPackages isEmpty
    ifTrue: [ self notify: 'Empty modified packages list'	"actually here for debugging purposes" ].
  modifiedPackages
    keysAndValuesDo: [ :packageName :repositoryGroup | 
      | gofer repositoryList packageSpec |
      gofer := Gofer new.
      gofer disablePackageCache.
      (repositoryGroup repositories reject: [ :repo | repo = cacheRepository ])
        do: [ :repository | gofer repository: repository ].
      gofer package: packageName.
      gofer commit: commitMessage ].
  ^ modifiedPackages