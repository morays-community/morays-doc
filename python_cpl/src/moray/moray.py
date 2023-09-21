#import argparse
import logging
import pycpl

def main():

    logging.basicConfig(filename='moray.log',encoding='utf-8',level=logging.INFO)
    pycpl.pycpl()  
 
if __name__=="__main__":
   main() 
