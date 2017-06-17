baseline
baseline: spec
  <baseline>
  spec
    for: #(common)
    do: [ 
      spec
        package: 'LibGit-Tests' with: [ spec requires: 'LibGit-Core' ];
        package: 'LibGit-GT' with: [ spec requires: 'LibGit-Core' ];
        package: 'LibGit-Patches' with: [ spec requires: 'LibGit-Core' ];
        package: 'LibGit-Core'.
      spec
        group: 'default'
        with: #('LibGit-Core').
		spec
			group: 'development'
			with: #('default' 'LibGit-Tests' 'LibGit-Patches' 'LibGit-GT').
		spec
			group: 'tests'
			with: #('LibGit-Tests').
		spec postLoadDoIt: #postLoadActions ]
