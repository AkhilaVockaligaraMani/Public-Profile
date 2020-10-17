package com.ext.lucene;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import org.apache.lucene.document.Document;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.store.FSDirectory;
import org.jsoup.Jsoup;
import org.apache.lucene.search.similarities.BM25Similarity;
import org.apache.lucene.search.similarities.ClassicSimilarity;
import org.apache.lucene.search.TopDocs;
/*
 * Author: Akhila Vockaligara Mani
 * This class invokes the method for searching the query in the indexed files
 * Also lists the ranks,score,filename,path,title for the documents found 
 * Uses the [OK]/[VS] ranking models
 */
public class MyQueryParser {
	/**
	    * searchIndexWithQueryParser searches the query in the indexed files.
	 	* @param String indexfilepath : path to store indexes
	 	* @param String userQuery : Query to be searched
	 	* @param String rankingModel : user input[OK]/[VS]
	 	* @throws IOException
	 	* @throws ParseException
	 	* @return void
	    */
	public void searchIndexWithQueryParser(String indexfilepath,String userQuery,String rankingModel) throws IOException,ParseException{
		SearchFields searchFields = new SearchFields();
		Query query =null;
		int hitsPerPage = 20000;
		int top10Docs;
		File indexDirectory = new File(indexfilepath);
		FSDirectory dir = FSDirectory.open(indexDirectory.toPath());
		if (!indexDirectory.exists()){
	        System.out.println("Sorry!This path" +dir.getDirectory()+ " does not exist or might be invalid");
	        System.exit(1);
	    }
		IndexReader readerIndex = DirectoryReader.open(dir);
		IndexSearcher searcherIndex = new IndexSearcher(readerIndex);
		Analyzer analyzer = new MyStemmingAnalyzer();
		//Search multiple fields concurrently
		MultiFieldQueryParser parser  = new MultiFieldQueryParser(searchFields.getSearchFields(), analyzer);
		parser.setAllowLeadingWildcard(true);
		try {
			query = parser.parse(userQuery);
			//Fetch the results based on the ranking model chosen
			if(rankingModel.equals("OK")){
    			searcherIndex.setSimilarity(new BM25Similarity());
    		}
    		else{
    			searcherIndex.setSimilarity(new ClassicSimilarity());
    		}	
		} catch (org.apache.lucene.queryparser.classic.ParseException e){
			System.out.println("Sorry! query cannot be parsed");
		}
		System.out.println("Started the search process for the query:  " + query);
        TopDocs results = searcherIndex.search(query, hitsPerPage);
	    ScoreDoc[] hitscounts = results.scoreDocs;
	    //get atmost 10 top documents
	    if(hitscounts.length > 10){
	    	top10Docs = 10;
		}
		else{
			top10Docs = hitscounts.length;
		}
        System.out.println("Found " + hitscounts.length + " hits."); 
        System.out.println("rank|score|fileName|docPath|Title");
        for (int i = 0; i <top10Docs; i++){
        	int j = i+1;
    	   ScoreDoc sd = hitscounts[i];
           float score = sd.score;
           //int docId = hitscounts[i].doc;
           System.out.printf("%3d"+"|"+"%4.2f"+"|", j, score);
           Document doc = searcherIndex.doc(hitscounts[i].doc);
	       String filepath = doc.get(LuceneConstants.FILE_PATH);
	        if (filepath != null){
	        	  File currentFile = new File(filepath);
		          System.out.print(currentFile.getName());
		          System.out.print("|"+ filepath);
		          //Using Jsoup parser to parse the HTML content to fetch title and summary of the document
	    	      org.jsoup.nodes.Document parseDoc = Jsoup.parse(currentFile,"UTF-8","");
		          String title = parseDoc.title();
		          //System.out.println(" "+title);
		          if (title != null && title != "" && !title.equals(null)){
		        	  System.out.print("|"+title+"\n");
		          }
		          else {
		        	  System.out.print("| \n");
		          }
	        } 
	        else {
	          System.out.println((i+1) + ". " + "No path for this document");
	        } 
       }    
        try {
			readerIndex.close();	
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}

}