I represent basic privacy question about collecting user activities and other data that helps to improve Pharo and related tools.

Responsibility: 
I keep information about sending diagnostic and usage data. 

Collaborators:
I only keep the privacy information. All services that collects diagnostic and usage data should do it only if #sendDiagnosticsAndUsageData is true. 

If you want to add more specific settings related to privacy, you can put it to the privacy group.

Public API and Key Messages

- sendDiagnosticsAndUsageData   
