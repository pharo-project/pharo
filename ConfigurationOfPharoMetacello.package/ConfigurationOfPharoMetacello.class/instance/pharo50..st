releases
pharo50: spec
   <version: 'pharo50'>

   spec
      for: #'pharo5.x'
      do: [ 
         spec
            baseline: 'Metacello'
            with: [ 
					spec 
						repository: 'github://pharo-project/pharo-metacello:pharo50/repository';
						loads: #('Metacello-TestsCore' 'Metacello-TestsReference' 'Metacello-TestsMCCore' 'Tutorial') ] ]