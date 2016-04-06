# How to use #

The implementation of the complex type extension to pyang is a python module available at $PYANG-CT\_ROOT/pyang/plugins/complex-types.py. Test cases for the extension can be found in the $PYANG-CT\_ROOT/test/test\_linowski\_ext directory.

By default the pyang complex type extension is disabled. In order to enable it, the '--enable-complex-types' command line argument should be used. For example, if you want to validate a module 'test-module.yang' with enabled complex type extension use the following command line:

pyang --enable-complex-types test-module.yang