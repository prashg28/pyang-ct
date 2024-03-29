"""pyang plugin handling"""

import os
import sys
import string

plugins = []
"""List of registered PyangPlugin instances"""

def init(plugindirs=[]):
    """Initialize the plugin framework"""

    # initialize the builtin plugins
    from translators import yang,yin,cts,xsd,xsd_ct
    yang.pyang_plugin_init()
    yin.pyang_plugin_init()
    cts.pyang_plugin_init()
    xsd.pyang_plugin_init()
    xsd_ct.pyang_plugin_init()

    # search for plugins in std directory
    basedir = os.path.split(sys.modules['pyang'].__file__)[0]
    plugindirs.insert(0, basedir + "/plugins")
    
    # add paths from env
    pluginpath = os.getenv('PYANG_PLUGINPATH')
    if pluginpath is not None:
        plugindirs.extend(string.split(pluginpath, os.pathsep))

    syspath = sys.path
    for plugindir in plugindirs:
        sys.path = [plugindir] + syspath
        fnames = os.listdir(plugindir)
        for fname in fnames:
            if fname.endswith(".py") and fname != '__init__.py':
                pluginmod = __import__(fname[:-3])
                try:
                    pluginmod.pyang_plugin_init()
                except AttributeError, s:
                    print pluginmod.__dict__
                    raise AttributeError, pluginmod.__file__ + ': ' + str(s)
        sys.path = syspath


def register_plugin(plugin):
    """Call this to register a pyang plugin. See class PyangPlugin
    for more info.
    """
    plugins.append(plugin)

class PyangPlugin(object):
    """Abstract base class for pyang plugins

    A pyang plugin is a module found in the plugins directory of the
    pyang installation, or in the dynamic pluginpath.

    Such a module must export a function 'pyang_plugin_init()', which
    may call pyang.plugin.register_plugin() with an instance of a class
    derived from this class as argument.

    A plugin can extend the base pyang library functions, or the pyang
    front-end program, or both.
    """

    ## pyang front-end program methods

    def add_output_format(self, fmts):
        """Add an output format to the pyang program.

        `fmts` is a dict which maps the format name string to a plugin
        instance.

        Override this method and update `fmts` with the output format
        name.
        """
        return
    def add_opts(self, optparser):
        """Add command line options to the pyang program.

        Override this method and add the plugin related options as an
        option group.
        """
        return

    ## library methods

    def setup_ctx(self, ctx):
        """Modify the Context at setup time.

        Override this method to modify the Context before the module
        repository is accessed.
        """
        return

    def pre_validate(self, ctx, module):
        """Called before the module is validated"""
        return

    def post_validate(self, ctx, module):
        """Called after the module has been validated"""
        return

    def emit(self, ctx, module, writef):
        """Produce the plugin output.

        Override this method to perform the output conversion.
        `writef` is a function that takes one string to print as argument.

        Raise error.EmitError on failure.
        """
        return


    

    
