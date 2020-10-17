package com.ext.lucene;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.lucene.analysis.core.LowerCaseFilter;
import org.apache.lucene.analysis.en.PorterStemFilter;
import org.apache.lucene.analysis.standard.ClassicTokenizer;
import org.apache.lucene.analysis.standard.StandardFilter;
import org.apache.lucene.analysis.synonym.SynonymFilter;
import org.apache.lucene.analysis.synonym.SynonymMap;
import org.apache.lucene.util.CharsRef;
/*
 * Author: Akhila Vockaligara Mani
 * This class invokes the method for setting the filters for an analyzer-
 * eg: PorterStemmer,LowerCaseFilter,SynonymFilter
 */
@SuppressWarnings("deprecation")
public class MyStemmingAnalyzer extends Analyzer{
	/**
 	 * Extends Analyzer to include PorterStemmer,LowerCase
 	 * @param file : file to Analyze
 	 * @return Analyzer.TokenStreamComponents
 	 */
	@Override
	protected Analyzer.TokenStreamComponents createComponents(String fieldName) {
		Tokenizer source = new ClassicTokenizer();
		TokenStream filter = new StandardFilter(source);
		filter = new LowerCaseFilter(filter);
		filter = new SynonymFilter(filter,getSynonymsMap(),false);
		return new Analyzer.TokenStreamComponents(source,new PorterStemFilter(filter));
	}
	/**
 	 * fetch the synonyms for a word in query string
 	 */
	private SynonymMap getSynonymsMap(){
		try {
			SynonymMap.Builder builder = new SynonymMap.Builder(true);
			builder.add(new CharsRef("finished"), new CharsRef("completed"),true);
			builder.add(new CharsRef("finished"), new CharsRef("over"),true);
			builder.add(new CharsRef("finished"), new CharsRef("ended"),true);
			builder.add(new CharsRef("happy"), new CharsRef("gay"),true);
			builder.add(new CharsRef("happy"), new CharsRef("bliss"),true);
			builder.add(new CharsRef("happy"), new CharsRef("joy"),true);
			SynonymMap mySynonymMap = builder.build(); 
			return mySynonymMap;
		}
		catch(Exception e) {
			e.printStackTrace();
			return null;
		}
		
	}
}