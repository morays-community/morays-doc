#import argparse
import logging
from smart_morays.pycpl import pycpl

def main():

    logging.basicConfig(filename='morays.log',encoding='utf-8',level=logging.INFO)
    pycpl()  
 
if __name__=="__main__":
   main() 
