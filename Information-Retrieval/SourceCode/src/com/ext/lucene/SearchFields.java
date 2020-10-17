package com.ext.lucene;
import java.io.IOException;
import java.io.InputStream;
import java.util.Scanner;
/*
 * Author: Akhila Vockaligara Mani
 * This class invokes the method for searching multiple fields concurrently
 * The fields are fetched from a Properties.txt file
 * The fields are comma separated values where the last item ends with a comma-
 * example: content,body,title,
 */
public class SearchFields {
	private Scanner scanner1;
	private Scanner scanner2;

	/**
	 * read the properties file to search multiple fields concurrently
	 * @return String array
	 * @throws IOException
	 */
	public String[] getSearchFields() throws IOException {
		int n = 0;
		InputStream is = getClass().getResourceAsStream("/properties/Properties.txt");
		InputStream in = getClass().getResourceAsStream("/properties/Properties.txt");
		scanner1 = new Scanner(is);
		scanner2 = new Scanner(in);
		scanner1.useDelimiter(",");
		scanner2.useDelimiter(",");
		while(scanner1.hasNext()) {
			n = n+1;
			scanner1.next();
		    }
		String[] allfields = new String[n];
		int i=0; 
		while(scanner2.hasNext()){
			String element = scanner2.next();
    	    allfields[i] = element;
    	    i++;
	    	}
	    scanner1.close();
	    scanner2.close();
	    return allfields;
	    }
	}
