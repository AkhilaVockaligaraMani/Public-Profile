package com.ext.lucene;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.ParseException;
import java.util.Scanner;

import org.apache.lucene.index.CorruptIndexException;
/*
 * Author: Akhila Vockaligara Mani
 * This class invokes the method for indexing
 * The variables/attributes used in the class are-
 * 'inputDocumentsPath': of type String- the path of the document collection;
 * 'indexFilePath' : of type String- the path where indexed documents are to be stored;
 * 'rankingModel' : of type String- the selected ranking model(VS/OK) {VS-Vector Space Model, OK- Okapi BM25}
 * 'inputquery' : of type String- search query;
 */
public class ProcessQueries {
	public static String inputDocumentsPath = "";
	public static String indexFilePath = "";
	public static String rankingModel = "";
	public static String inputquery = "";
	 public static void main(String[] args) throws FileNotFoundException,CorruptIndexException, IOException, ParseException, org.apache.lucene.queryparser.classic.ParseException 
     {
		 if(args.length < 4) {
			 System.out.println("Please provide the input in the below order");
			 System.out.println("[path to document folder] [path to index folder] [VS/OK] [query]");
		 }
		 else {
			 inputDocumentsPath = args[0];
			 indexFilePath = args[1];
			 rankingModel = args[2];
			 System.out.println("Path of document collection:\t"+inputDocumentsPath+
					 "\npath to index folder:\t"+indexFilePath+"\nRanking Model Chosen:\t"+rankingModel);
				if(args.length == 4){
					inputquery = args[3];
				}
				//concatenate the arguments from arg[4] as a query string
				else if(args.length > 4){
					for(int i=3; i<args.length; i++){
						inputquery += args[i]+" ";
					}
				}
				System.out.println("Query:\t"+inputquery);
		 }
		 if((!rankingModel.equalsIgnoreCase("OK"))&&(!rankingModel.equalsIgnoreCase("VS"))){
			 System.out.println("Please Check the Ranking Model Chosen\n"+"Choose [VS] or [OK]");
			 System.exit(0);
		 }
		 Scanner systemscanner = new Scanner(System.in);
  	     System.out.print("Select one of the below options for indexing\n");
  	     System.out.print("1.create\t 2.update\n");
  	     //Let the user choose the indexing option create/update
  	     String createorupdate = systemscanner.next();
  	     systemscanner.close();
  	     long startTime = System.currentTimeMillis();
  	     //Indexing functionality
  	     Indexer indexer = new Indexer();
  	     indexer.createIndex(inputDocumentsPath,indexFilePath,createorupdate);
  	     //Search functionality
  	     MyQueryParser queryParser = new MyQueryParser();
  	     queryParser.searchIndexWithQueryParser(indexFilePath,inputquery,rankingModel);
  	     long endTime = System.currentTimeMillis();
		 System.out.println("Total time Taken in milliseconds: "+(endTime-startTime));
     }
}
