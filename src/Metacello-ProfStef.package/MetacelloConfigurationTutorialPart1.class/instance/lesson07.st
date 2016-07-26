lessons
lesson07
	^Lesson
		title: 'Lesson 7'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #baseline07:.
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version07:.

"For version 0.7, we are ceating a baseline version specification which is expected to be used
across several versions and the version specification which is restricted to the file versions.

In method #baseline07: the structure of version ''0.7-baseline'' is specified. The repository is 
listed, the packages are listed and the required packages are defined. We''ll cover the #blessing:
in a later lesson.

In method #version07: the file versions are specified. You will note that the pragma as an #imports: component that specifies the list of versions that this version (version ''0.7'') is based upon. In 
fact, if you print the spec for ''0.7-baseline'' and then print the spec for ''0.7'' you can see that 
''0.7'' is a composition of both versions:"

   (MetacelloTutorialConfig project version: ''0.7-baseline'') spec.
   (MetacelloTutorialConfig project version: ''0.7'') spec.

"Of course if you print the ''0.6'' spec and the ''0.7'' spec you can see that they specify exactly the
same information in a slightly different way:"

  (MetacelloTutorialConfig project version: ''0.6'') spec.
  (MetacelloTutorialConfig project version: ''0.7'') spec.

"and if you load each of the versions, you will see that they load the same packages, in the same 
order:"

  (MetacelloTutorialConfig project version: ''0.6'') load.
  (MetacelloTutorialConfig project version: ''0.7'') load.

"Finally, even though version ''0.7-baseline'' does not have explicit package versions, you may load
the version. When the ''real'' loader encounters a package name (without version information) it will
attempt to load the latest version of the package from the repository. With the
MetacelloNullRecordingMCSpecLoader the packages names are ''loaded'':"

  (MetacelloTutorialConfig project version: ''0.7-baseline'') load.

"Of course when a number of developers are working on a project it may be useful to load a
#baseline version so that you get the latest work from all of the project members. "

ProfStef next.
'