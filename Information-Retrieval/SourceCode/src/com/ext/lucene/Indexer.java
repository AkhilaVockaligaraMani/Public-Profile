package com.ext.lucene;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.Term;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.store.FSDirectory;
/*
 * Author: Akhila Vockaligara Mani
 * This class invokes the methods for checking the file formats(.txt,.html) and
 * indexing mode(create/update), writing index to a specific location
 */
public class Indexer 
{
 public IndexWriter writer = null;
 public File indexDirectory = null;
 public File sourceDirectory = null;
 public static int count = 0;
 /**
	 * invokes createIndexWriter
	 * @param String sourceFilePath : file to index
	 * @param String indexFilePath : path to store index
	 * @param String createorupdate : index mode(user input)
	 * @throws FileNotFoundException
	 * @throws CorruptIndexException
	 * @throws IOException
	 * @throws ParseException
	 * @return void
	 */
 	public void createIndex(String sourceFilePath,String indexFilePath,String createorupdate) throws FileNotFoundException, CorruptIndexException, IOException, ParseException, org.apache.lucene.queryparser.classic.ParseException 
 	{ 
 		if(createorupdate.equalsIgnoreCase("create")||createorupdate.equalsIgnoreCase("update")) {
		createIndexWriter(indexFilePath,createorupdate,sourceFilePath);
		closeIndexWriter(); 
		}
		else {
			System.out.println("Invalid Options Chosen for Indexing");
			System.exit(1);
		}
		System.out.println("Total no of files "+createorupdate+"d: "+count);
 		
 	}
 	
 /**
    * createIndexWriter sets the mode for an index writer based on user input.
 	* @param String indexFilePath : path to store indexes
 	* @param String createorupdate : index mode
 	* @param String sourceFilePath : input file
 	* @return void
    */
 	public void createIndexWriter(String indexFilePath,String createorupdate,String sourceFilePath) 
 	{
 		try 
 		{
 			indexDirectory = new File(indexFilePath);
 			sourceDirectory = new File(sourceFilePath);
 			if (!indexDirectory.exists()){  
 				indexDirectory.mkdir();  
 			} 
 			if (!sourceDirectory.exists()){  
 				System.out.println("Please provide the data file for indexing"); 
 				System.exit(0);
 			} 
 			FSDirectory dir = FSDirectory.open(indexDirectory.toPath()); 
 			Analyzer analyzer = new MyStemmingAnalyzer(); 
 			IndexWriterConfig config = new IndexWriterConfig(analyzer);
 			writer = new IndexWriter(dir, config);
 			if (createorupdate.equalsIgnoreCase("create")) {
 				/*Create a new index in the directory*/
 				config.setOpenMode(OpenMode.CREATE);
 			} else{
 				/*Update existing index*/
 				config.setOpenMode(OpenMode.APPEND);
 			} 
 			System.out.println("List of files parsed and indexed:");
 			checkFileValidity(sourceFilePath);
 		} 
 		catch (Exception ex){ 
 			System.out.println("Sorry cannot get the index writer");
 		}
 	}
 	
  /**
 	 * check for text and html files recursively in a directory
 	 * @param String file : file to index
 	 * @return void
 	 */
 	public void checkFileValidity(String sourceFilePath) 
 		{
 		File[] filesToIndex = new File[200000]; // suppose there are 200000 files at max 
 		filesToIndex = new File(sourceFilePath).listFiles(); 
 		if(filesToIndex.equals(null))
 		System.out.println("Please provide the data for indexing");
 		else {
 		for (File file : filesToIndex) 
 		{
 			try 
 			{ //to check whether the file is a directory or not.
 				if(file.isDirectory()){
 					checkFileValidity(file.toString());
 				}
 				else {
 					if (!file.isHidden() && file.exists() && file.canRead() && file.length() > 0.0 
					 && file.isFile() && file.getName().toLowerCase().endsWith(".txt")) {
 						indexAllFiles(file);
 					}
 					else if (!file.isHidden() && file.exists() && file.canRead() && file.length() > 0.0 
 					&& file.isFile() && file.getName().toLowerCase().endsWith(".html")){
 						indexAllFiles(file);
 					}
 					else {
 						System.out.println(" Not a valid file format ");
 	 				}
 				}
 			} 
 			catch (Exception e) {
 				System.out.println("Sorry cannot index " + file.getAbsolutePath());
 			}
 			}
 		}
 		}
 	
 /**
 	 * writes/update index files
 	 * @param File file : file to index
 	 * @throws CorruptIndexException
 	 * @throws IOException
 	 * @return void
 	 */
 	public void indexAllFiles(File file) throws CorruptIndexException, IOException 
 	{   Document doc = new Document(); 
 		doc.add(new TextField(LuceneConstants.FILE_CONTENT, new FileReader(file)));
 		doc.add(new StringField(LuceneConstants.FILE_NAME, file.getName(),Field.Store.YES));
 		doc.add(new StringField(LuceneConstants.FILE_PATH,file.toString(),Field.Store.YES));
 		if (doc != null) {
 		if (writer.getConfig().getOpenMode() == OpenMode.CREATE) {
			writer.addDocument(doc);
			count++;		
		} else {
			writer.updateDocument(new Term(LuceneConstants.FILE_NAME,
		    file.getName()),doc);
			count++;
		  }
 		}
 		System.out.println(file.getName());
 	}
	// Close the IndexWriter
 	public void closeIndexWriter() 
 	{
 		try{ 
 			writer.close();
 		}
 		catch (Exception e) {
 			System.out.println("Indexer Cannot be closed");
 		}
 	}

}
