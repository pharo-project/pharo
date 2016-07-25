Copy me to create a new configuration or edit and evaluate the following doits.

        "Create configuration class and initial baseline method"

        MetacelloToolBox 
                createBaseline: '1.0-baseline'
                for: 'MyProject'
                repository: 'http://www.example.com/MyProjectRepository'
                requiredProjects: #('Gofer')
                packages: #('MyProject-Core' 'MyProject-Tests')
                dependencies:
                        {('MyProject-Core' -> #('Gofer')).
                         ('MyProject-Tests' -> #('MyProject-Core'))}
                groups:
                        {('default' -> #('Core')).
                        ('Core' -> #('MyProject-Core')).
                        ('Tests' -> #('MyProject-Tests'))}.

	   "create initial development method from the baseline"

         MetacelloToolBox
               createDevelopment: '1.0'
               for: 'MyProject'
                importFromBaseline: '1.0-baseline'
                description: 'initial version'.
