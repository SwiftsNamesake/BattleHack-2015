#
# build.py
# Because typing commands over and over is for chumps
#
# Jonatan H Sundqvist
# September 19 2015
#

# TODO | - Documentation
#        - Binary dependencies (DLL, lib)
#        - Options
#        - Package updates
#        - Unit tests
#        - Benchmarks, debug builds
#        - Hoogle

#
# SPEC | -
#        -



import argparse
import sys
import subprocess



def main():

    '''
    Docstring goes here

    '''

    # TODO: Make sure we cd to the right directory
    subprocess.call('cabal update')
    subprocess.call('cabal install --only-dependencies --enable-documentation')
    subprocess.call('cabal haddock --executables')
    subprocess.call('cabal install')

    if sys.argv[1] == 'run':
        subprocess.call('cabal run')



if __name__ == '__main__':
    main()
