I display code critics, the results obtained when running a lint rule.

Example: 


| rule env |
rule :=  RBExcessiveArgumentsRule new.
env := (RBPackageEnvironment packageName: 'Manifest-Core').

(CriticBrowser openOnRule: rule onEnvironment: env).